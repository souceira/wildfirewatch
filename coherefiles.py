# import libraries
import cohere

# if evacuating:
def handle_click():
  co = cohere.Client('DOZ4gBc3BoaXXzM1QbxMUheta4YErWOFvrEyXzjL') # This is your trial API key
  response = co.generate(
    model='command-xlarge-nightly',
    prompt='Write down a list of emergency supplies and the most important items to gather in case you need to evacuate because of a wildfire, and write it like a short blog post.',
    max_tokens=450,
    temperature=0.2,
    k=0,
    p=0.75,
    frequency_penalty=0,
    presence_penalty=0,
    stop_sequences=[],
    return_likelihoods='NONE')
  print('{}'.format(response.generations[0].text))